package com.atorres.nttdata.prodpasivems.repository.accountstrategy;

import com.atorres.nttdata.prodpasivems.model.clientms.ClientType;
import org.springframework.stereotype.Component;

import java.util.EnumMap;
import java.util.Map;

@Component
public class AccountStrategyFactory {
    private final Map<ClientType, AccountStrategy> strategies = new EnumMap<>(ClientType.class);

    public AccountStrategyFactory() {
        initStrategies();
    }

    public AccountStrategy getStrategy(ClientType userType) {
        if (userType == null || !strategies.containsKey(userType)) {
            throw new IllegalArgumentException("Invalid " + userType);
        }
        return strategies.get(userType);
    }

    private void initStrategies() {
        strategies.put(ClientType.PERSONAL, new PersonalAccountStrategy());
        strategies.put(ClientType.BUSSINES, new BussinesAccountStrategy());
    }
}
